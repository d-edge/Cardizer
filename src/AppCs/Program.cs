using System;
using Dedge;
namespace AppCs
{
    class Program
    {
        static void Main(string[] args)
        {
            var card = new Cardizer().NextVisa();
            Console.WriteLine(card);
        }
    }
}
